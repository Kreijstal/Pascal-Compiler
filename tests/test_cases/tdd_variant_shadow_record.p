program tdd_variant_shadow_record;

{$mode objfpc}

type
  Variant = record
    Value: PtrInt;
  end;

var
  Sample: Variant;

begin
  Sample.Value := 42;
end.
