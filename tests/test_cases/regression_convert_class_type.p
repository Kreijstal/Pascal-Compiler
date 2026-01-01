program RegressionConvertClassType;

type
  TCounter = class
  private
    FValue: Integer;
  public
    constructor Create(Start: Integer);
    function Next: Integer;
  end;

constructor TCounter.Create(Start: Integer);
begin
  FValue := Start;
end;

function TCounter.Next: Integer;
begin
  Inc(FValue);
  Result := FValue;
end;

var
  Counter: TCounter;
begin
  Counter := TCounter.Create(6);
  WriteLn(Counter.Next);
end.
