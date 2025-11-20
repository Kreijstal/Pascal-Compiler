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
  { 
    FIX: Removed FillChar. 
    1. The compiler initialization (calloc) already zeroes the instance.
    2. FillChar(Counter, ...) on a class wipes the pointer, not the instance.
  }
  
  Counter.Value := 7;
  Counter.Value := 7;
  Counter.Value := 10;
  WriteLn('Value=', Counter.Value);
  WriteLn('Changes=', Counter.Changes);
end.
