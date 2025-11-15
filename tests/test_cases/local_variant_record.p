program LocalVariantRecord;

procedure Demo;
type
  TLocal = record
    Prefix: integer;
    case Flag: boolean of
      true: (PositiveValue: integer);
      false: (NegativeValue: integer);
  end;
var
  Value: TLocal;
begin
  Value.Flag := true;
  Value.Prefix := 10;
  Value.PositiveValue := 32;
  writeln('Prefix=', Value.Prefix);
  writeln('Positive=', Value.PositiveValue);
end;

begin
  Demo;
end.
