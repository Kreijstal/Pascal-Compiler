{ TDD: Open array (dynamic array) typed constant
  Pattern from FPC compiler: Families : array of TFamilies = ((k:'SAMD21'; v:$68ed2b88), ...)
  Error: "Open array typed const Families is not supported" }
program tdd_open_array_typed_const;

type
  TFamily = record
    Name: string[20];
    Code: LongWord;
  end;

const
  Families: array of TFamily = (
    (Name: 'Alpha'; Code: $DEADBEEF),
    (Name: 'Beta'; Code: $CAFEBABE),
    (Name: 'Gamma'; Code: $12345678)
  );

var
  I: Integer;
begin
  WriteLn(Length(Families));
  for I := 0 to High(Families) do
    WriteLn(Families[I].Name, ' ', Families[I].Code);
end.
