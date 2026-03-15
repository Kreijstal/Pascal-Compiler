unit reg_imported_standalone_operator_unit;
{$mode objfpc}

interface

type
  TCounter = record
    Value: LongInt;
  end;

operator - (const a, b: TCounter): TCounter;

implementation

operator - (const a, b: TCounter): TCounter;
begin
  Result.Value := a.Value - b.Value;
end;

end.
