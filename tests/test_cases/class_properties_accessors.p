program ClassPropertiesAccessors;

type
  TCounter = class
    FValue: Integer;
    FChanges: Integer;
    procedure SetValue(const NewValue: Integer);
    property Value: Integer read FValue write SetValue;
    property Changes: Integer read FChanges;
  end;

procedure TCounter.SetValue(const NewValue: Integer);
begin
  if FValue <> NewValue then
  begin
    FValue := NewValue;
    Inc(FChanges);
  end;
end;

var
  Counter: TCounter;
begin
  FillChar(Counter, SizeOf(Counter), 0);
  Counter.Value := 7;
  Counter.Value := 7;
  Counter.Value := 10;
  WriteLn('Value=', Counter.Value);
  WriteLn('Changes=', Counter.Changes);
end.
